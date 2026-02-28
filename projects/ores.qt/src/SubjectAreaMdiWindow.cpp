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
#include "ores.qt/SubjectAreaMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QtConcurrent>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/EntityItemDelegate.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.dq/messaging/data_organization_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

SubjectAreaMdiWindow::SubjectAreaMdiWindow(
    ClientManager* clientManager, const QString& username, QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      username_(username),
      model_(new ClientSubjectAreaModel(clientManager, this)),
      proxyModel_(new QSortFilterProxyModel(this)),
      tableView_(new QTableView(this)),
      toolbar_(nullptr) {

    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    setupUi();
    setupToolbar();
    setupConnections();

    model_->refresh();
}

void SubjectAreaMdiWindow::setupUi() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);

    tableView_->setModel(proxyModel_);
    tableView_->setSortingEnabled(true);
    tableView_->setItemDelegate(new EntityItemDelegate(
        ClientSubjectAreaModel::columnStyles(), tableView_));
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    tableView_->setAlternatingRowColors(true);
    tableView_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    tableView_->verticalHeader()->setVisible(false);
    tableView_->sortByColumn(ClientSubjectAreaModel::Name, Qt::AscendingOrder);

    initializeTableSettings(tableView_, model_,
        ClientSubjectAreaModel::kSettingsGroup,
        ClientSubjectAreaModel::defaultHiddenColumns(), ClientSubjectAreaModel::kDefaultWindowSize, 1);

    layout->addWidget(tableView_);
}

void SubjectAreaMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new subject area"));

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected subject area"));
    editAction_->setEnabled(false);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected subject area(s)"));
    deleteAction_->setEnabled(false);

    toolbar_->addSeparator();

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::History, IconUtils::DefaultIconColor),
        tr("History"));
    historyAction_->setToolTip(tr("View version history"));
    historyAction_->setEnabled(false);

    toolbar_->addSeparator();

    refreshAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::ArrowSync, IconUtils::DefaultIconColor),
        tr("Refresh"));
    refreshAction_->setToolTip(tr("Refresh subject areas"));
    initializeStaleIndicator(refreshAction_, IconUtils::iconPath(Icon::ArrowSync));

    if (auto* layout = qobject_cast<QVBoxLayout*>(this->layout())) {
        layout->insertWidget(0, toolbar_);
    }
}

void SubjectAreaMdiWindow::setupConnections() {
    connect(model_, &ClientSubjectAreaModel::dataLoaded,
            this, &SubjectAreaMdiWindow::onDataLoaded);
    connect(model_, &ClientSubjectAreaModel::loadError,
            this, &SubjectAreaMdiWindow::onLoadError);
    connect(tableView_, &QTableView::doubleClicked,
            this, &SubjectAreaMdiWindow::onRowDoubleClicked);
    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &SubjectAreaMdiWindow::onSelectionChanged);

    connect(addAction_, &QAction::triggered, this, &SubjectAreaMdiWindow::onAddClicked);
    connect(editAction_, &QAction::triggered, this, &SubjectAreaMdiWindow::onEditClicked);
    connect(deleteAction_, &QAction::triggered, this, &SubjectAreaMdiWindow::onDeleteClicked);
    connect(refreshAction_, &QAction::triggered, this, &SubjectAreaMdiWindow::onRefreshClicked);
    connect(historyAction_, &QAction::triggered, this, &SubjectAreaMdiWindow::onHistoryClicked);
}

void SubjectAreaMdiWindow::onDataLoaded() {
    emit statusChanged(tr("Loaded %1 subject areas").arg(model_->rowCount()));
    updateActionStates();
}

void SubjectAreaMdiWindow::onLoadError(const QString& error_message,
                                        const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void SubjectAreaMdiWindow::onRowDoubleClicked(const QModelIndex& index) {
    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* subject_area = model_->getSubjectArea(sourceIndex.row())) {
        emit showSubjectAreaDetails(*subject_area);
    }
}

void SubjectAreaMdiWindow::onAddClicked() {
    emit addNewRequested();
}

void SubjectAreaMdiWindow::onEditClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* subject_area = model_->getSubjectArea(sourceIndex.row())) {
        emit showSubjectAreaDetails(*subject_area);
    }
}

void SubjectAreaMdiWindow::onDeleteClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    std::vector<dq::messaging::subject_area_key> keys;
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* subject_area = model_->getSubjectArea(sourceIndex.row())) {
            dq::messaging::subject_area_key key;
            key.name = subject_area->name;
            key.domain_name = subject_area->domain_name;
            keys.push_back(key);
        }
    }

    QString message = keys.size() == 1
        ? tr("Delete subject area '%1'?").arg(QString::fromStdString(keys[0].name))
        : tr("Delete %1 subject areas?").arg(keys.size());

    auto reply = MessageBoxHelper::question(this, tr("Confirm Delete"), message,
                                            QMessageBox::Yes | QMessageBox::No);
    if (reply != QMessageBox::Yes) return;

    QPointer<SubjectAreaMdiWindow> self = this;
    auto task = [self, keys = std::move(keys)]() -> bool {
        if (!self || !self->clientManager_) return false;

        dq::messaging::delete_subject_area_request request;
        request.keys = keys;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_subject_area_request,
            0, std::move(payload));

        auto response_result = self->clientManager_->sendRequest(std::move(request_frame));
        if (!response_result) return false;

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return false;

        auto response = dq::messaging::delete_subject_area_response::deserialize(*payload_result);
        return response && response->success;
    };

    auto* watcher = new QFutureWatcher<bool>(this);
    connect(watcher, &QFutureWatcher<bool>::finished, this, [self, watcher]() {
        bool success = watcher->result();
        watcher->deleteLater();

        if (self) {
            if (success) {
                emit self->statusChanged(tr("Subject area(s) deleted successfully"));
                self->reload();
            } else {
                emit self->errorOccurred(tr("Failed to delete subject area(s)"));
            }
        }
    });

    watcher->setFuture(QtConcurrent::run(task));
}

void SubjectAreaMdiWindow::onRefreshClicked() {
    emit statusChanged(tr("Refreshing..."));
    model_->refresh();
}

void SubjectAreaMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void SubjectAreaMdiWindow::onHistoryClicked() {
    auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) return;

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* subject_area = model_->getSubjectArea(sourceIndex.row())) {
        emit showSubjectAreaHistory(QString::fromStdString(subject_area->name),
                                    QString::fromStdString(subject_area->domain_name));
    }
}

void SubjectAreaMdiWindow::updateActionStates() {
    auto selected = tableView_->selectionModel()->selectedRows();
    bool hasSelection = !selected.isEmpty();
    bool singleSelection = selected.size() == 1;

    editAction_->setEnabled(singleSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(singleSelection);
}

void SubjectAreaMdiWindow::reload() {
    clearStaleIndicator();
    model_->refresh();
}

}
