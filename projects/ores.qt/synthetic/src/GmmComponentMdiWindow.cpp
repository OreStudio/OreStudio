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
#include "ores.qt/GmmComponentMdiWindow.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include <QFutureWatcher>
#include <QHeaderView>
#include <QMessageBox>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

GmmComponentMdiWindow::GmmComponentMdiWindow(ClientManager* clientManager,
                                             const QString& username,
                                             QWidget* parent)
    : EntityListMdiWindow(parent)
    , clientManager_(clientManager)
    , username_(username)
    , toolbar_(nullptr)
    , tableView_(nullptr)
    , model_(nullptr)
    , proxyModel_(nullptr)
    , paginationWidget_(nullptr)
    , reloadAction_(nullptr)
    , addAction_(nullptr)
    , editAction_(nullptr)
    , deleteAction_(nullptr) {

    setupUi();
    setupConnections();
    reload();
}

void GmmComponentMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);
    layout->addWidget(loadingBar());

    setupTable();
    layout->addWidget(tableView_);

    paginationWidget_ = new PaginationWidget(this);
    layout->addWidget(paginationWidget_);
}

void GmmComponentMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered, this, &EntityListMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor), tr("Add"));
    addAction_->setToolTip(tr("Add new GMM component"));
    connect(addAction_, &QAction::triggered, this, &GmmComponentMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Edit, IconUtils::DefaultIconColor), tr("Edit"));
    editAction_->setToolTip(tr("Edit selected GMM component"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this, &GmmComponentMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor), tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected GMM component"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_, &QAction::triggered, this, &GmmComponentMdiWindow::deleteSelected);
}

void GmmComponentMdiWindow::setupTable() {
    model_ = new ClientGmmComponentModel(clientManager_, this);
    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    tableView_ = new QTableView(this);
    tableView_->setModel(proxyModel_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setSortingEnabled(true);
    tableView_->setAlternatingRowColors(true);
    tableView_->verticalHeader()->setVisible(false);

    initializeTableSettings(tableView_,
                            model_,
                            "GmmComponentListWindow",
                            {ClientGmmComponentModel::Mean},
                            {900, 400},
                            1);
}

void GmmComponentMdiWindow::setupConnections() {
    connect(
        model_, &ClientGmmComponentModel::dataLoaded, this, &GmmComponentMdiWindow::onDataLoaded);
    connect(model_, &ClientGmmComponentModel::loadError, this, &GmmComponentMdiWindow::onLoadError);

    connect(tableView_->selectionModel(),
            &QItemSelectionModel::selectionChanged,
            this,
            &GmmComponentMdiWindow::onSelectionChanged);
    connect(tableView_, &QTableView::doubleClicked, this, &GmmComponentMdiWindow::onDoubleClicked);

    connect(
        paginationWidget_, &PaginationWidget::page_size_changed, this, [this](std::uint32_t size) {
            model_->set_page_size(size);
            model_->refresh();
        });

    connect(paginationWidget_, &PaginationWidget::load_all_requested, this, [this]() {
        const auto total = model_->total_available_count();
        if (total > 0 && total <= 1000) {
            model_->set_page_size(total);
            model_->refresh();
        }
    });

    connect(
        paginationWidget_,
        &PaginationWidget::page_requested,
        this,
        [this](std::uint32_t offset, std::uint32_t limit) { model_->load_page(offset, limit); });

    connectModel(model_);
}

void GmmComponentMdiWindow::doReload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading GMM components";
    clearStaleIndicator();
    emit statusChanged(tr("Loading GMM components..."));
    model_->refresh();
}

void GmmComponentMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    const auto total = model_->total_available_count();
    emit statusChanged(tr("Loaded %1 of %2 GMM components").arg(loaded).arg(total));

    paginationWidget_->update_state(loaded, total);
    paginationWidget_->set_load_all_enabled(loaded < static_cast<int>(total) && total > 0 &&
                                            total <= 1000);
}

void GmmComponentMdiWindow::onLoadError(const QString& error_message, const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void GmmComponentMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void GmmComponentMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* component = model_->getComponent(sourceIndex.row())) {
        emit showGmmComponentDetails(*component);
    }
}

void GmmComponentMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
}

void GmmComponentMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new GMM component requested";
    emit addNewRequested();
}

void GmmComponentMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* component = model_->getComponent(sourceIndex.row())) {
        emit showGmmComponentDetails(*component);
    }
}

void GmmComponentMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete GMM component while disconnected.");
        return;
    }

    std::vector<std::string> ids;
    std::vector<int> indices;
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* component = model_->getComponent(sourceIndex.row())) {
            ids.push_back(boost::uuids::to_string(component->id));
            indices.push_back(component->component_index);
        }
    }

    if (ids.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid GMM components to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << ids.size() << " GMM components";

    QString confirmMessage;
    if (ids.size() == 1) {
        confirmMessage =
            QString("Are you sure you want to delete GMM component %1?").arg(indices.front());
    } else {
        confirmMessage =
            QString("Are you sure you want to delete %1 GMM components?").arg(ids.size());
    }

    auto reply = MessageBoxHelper::question(
        this, "Delete GMM Component", confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<GmmComponentMdiWindow> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
        std::vector<std::string> ids;
    };

    auto task = [self, ids]() -> DeleteResult {
        if (!self || !self->clientManager_)
            return {false, "Window closed", {}};

        BOOST_LOG_SEV(lg(), debug)
            << "Making delete request for " << ids.size() << " GMM components";

        synthetic::messaging::delete_gmm_component_request request{.ids = ids};
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));
        if (!response_result) {
            return {false, "Failed to communicate with server", {}};
        }
        return {response_result->success, response_result->message, ids};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            for (const auto& id : result.ids) {
                BOOST_LOG_SEV(lg(), debug) << "GMM Component deleted: " << id;
                emit self->gmmComponentDeleted(QString::fromStdString(id));
            }
            self->model_->refresh();
            QString msg =
                result.ids.size() == 1 ?
                    "Successfully deleted 1 GMM component" :
                    QString("Successfully deleted %1 GMM components").arg(result.ids.size());
            emit self->statusChanged(msg);
        } else {
            BOOST_LOG_SEV(lg(), error) << "GMM Component deletion failed: " << result.message;
            QString msg = QString::fromStdString(result.message);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Delete Failed", msg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
