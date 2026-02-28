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
#include "ores.qt/OriginDimensionMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/EntityItemDelegate.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.dq/messaging/dimension_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

OriginDimensionMdiWindow::OriginDimensionMdiWindow(
    ClientManager* clientManager,
    const QString& username,
    QWidget* parent)
    : EntityListMdiWindow(parent),
      clientManager_(clientManager),
      username_(username),
      toolbar_(nullptr),
      tableView_(nullptr),
      model_(nullptr),
      proxyModel_(nullptr),
      reloadAction_(nullptr),
      addAction_(nullptr),
      editAction_(nullptr),
      deleteAction_(nullptr),
      historyAction_(nullptr) {

    setupUi();
    setupConnections();

    // Initial load
    reload();
}

void OriginDimensionMdiWindow::setupUi() {
    WidgetUtils::setupComboBoxes(this);
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);

    setupTable();
    layout->addWidget(tableView_);
}

void OriginDimensionMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered, this,
            &OriginDimensionMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Add, IconUtils::DefaultIconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new origin dimension"));
    connect(addAction_, &QAction::triggered, this,
            &OriginDimensionMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected origin dimension"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this,
            &OriginDimensionMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected origin dimension"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_, &QAction::triggered, this,
            &OriginDimensionMdiWindow::deleteSelected);

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::History, IconUtils::DefaultIconColor),
        tr("History"));
    historyAction_->setToolTip(tr("View origin dimension history"));
    historyAction_->setEnabled(false);
    connect(historyAction_, &QAction::triggered, this,
            &OriginDimensionMdiWindow::viewHistorySelected);
}

void OriginDimensionMdiWindow::setupTable() {
    model_ = new ClientOriginDimensionModel(clientManager_, this);
    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    tableView_ = new QTableView(this);
    tableView_->setModel(proxyModel_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setSortingEnabled(true);
    tableView_->setItemDelegate(new EntityItemDelegate(
        ClientOriginDimensionModel::columnStyles(), tableView_));
    tableView_->setAlternatingRowColors(true);
    tableView_->verticalHeader()->setVisible(false);

    initializeTableSettings(tableView_, model_,
        ClientOriginDimensionModel::kSettingsGroup,
        ClientOriginDimensionModel::defaultHiddenColumns(),
        ClientOriginDimensionModel::kDefaultWindowSize, 1);
}

void OriginDimensionMdiWindow::setupConnections() {
    connect(model_, &ClientOriginDimensionModel::dataLoaded,
            this, &OriginDimensionMdiWindow::onDataLoaded);
    connect(model_, &ClientOriginDimensionModel::loadError,
            this, &OriginDimensionMdiWindow::onLoadError);

    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &OriginDimensionMdiWindow::onSelectionChanged);
    connect(tableView_, &QTableView::doubleClicked,
            this, &OriginDimensionMdiWindow::onDoubleClicked);
}

void OriginDimensionMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading origin dimensions";
    clearStaleIndicator();
    emit statusChanged(tr("Loading origin dimensions..."));
    model_->refresh();
}

void OriginDimensionMdiWindow::onDataLoaded() {
    emit statusChanged(tr("Loaded %1 origin dimensions").arg(model_->rowCount()));
}

void OriginDimensionMdiWindow::onLoadError(const QString& error_message,
                                            const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void OriginDimensionMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void OriginDimensionMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* dimension = model_->getDimension(sourceIndex.row())) {
        emit showDimensionDetails(*dimension);
    }
}

void OriginDimensionMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
}

void OriginDimensionMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new origin dimension requested";
    emit addNewRequested();
}

void OriginDimensionMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* dimension = model_->getDimension(sourceIndex.row())) {
        emit showDimensionDetails(*dimension);
    }
}

void OriginDimensionMdiWindow::viewHistorySelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "View history requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* dimension = model_->getDimension(sourceIndex.row())) {
        BOOST_LOG_SEV(lg(), debug) << "Emitting showDimensionHistory for code: "
                                   << dimension->code;
        emit showDimensionHistory(QString::fromStdString(dimension->code));
    }
}

void OriginDimensionMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete origin dimension while disconnected.");
        return;
    }

    std::vector<std::string> codes;
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* dimension = model_->getDimension(sourceIndex.row())) {
            codes.push_back(dimension->code);
        }
    }

    if (codes.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid origin dimensions to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << codes.size()
                               << " origin dimensions";

    QString confirmMessage;
    if (codes.size() == 1) {
        confirmMessage = QString("Are you sure you want to delete origin dimension '%1'?")
            .arg(QString::fromStdString(codes.front()));
    } else {
        confirmMessage = QString("Are you sure you want to delete %1 origin dimensions?")
            .arg(codes.size());
    }

    auto reply = MessageBoxHelper::question(this, "Delete Origin Dimension",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<OriginDimensionMdiWindow> self = this;
    using DeleteResult = std::vector<std::pair<std::string, std::pair<bool, std::string>>>;

    auto task = [self, codes]() -> DeleteResult {
        DeleteResult results;
        if (!self) return {};

        BOOST_LOG_SEV(lg(), debug) << "Making batch delete request for "
                                   << codes.size() << " origin dimensions";

        dq::messaging::delete_origin_dimension_request request;
        request.codes = codes;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_origin_dimension_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to send batch delete request";
            for (const auto& code : codes) {
                results.push_back({code, {false, "Failed to communicate with server"}});
            }
            return results;
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to decompress batch response";
            for (const auto& code : codes) {
                results.push_back({code, {false, "Failed to decompress server response"}});
            }
            return results;
        }

        auto response = dq::messaging::delete_origin_dimension_response::
            deserialize(*payload_result);

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize batch response";
            for (const auto& code : codes) {
                results.push_back({code, {false, "Invalid server response"}});
            }
            return results;
        }

        for (const auto& code : codes) {
            results.push_back({code, {response->success, response->message}});
        }

        return results;
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, watcher]() {
        auto results = watcher->result();
        watcher->deleteLater();

        int success_count = 0;
        int failure_count = 0;
        QString first_error;

        for (const auto& [code, result] : results) {
            auto [success, message] = result;

            if (success) {
                BOOST_LOG_SEV(lg(), debug) << "Origin dimension deleted: " << code;
                success_count++;
                emit self->dimensionDeleted(QString::fromStdString(code));
            } else {
                BOOST_LOG_SEV(lg(), error) << "Origin dimension deletion failed: "
                                           << code << " - " << message;
                failure_count++;
                if (first_error.isEmpty()) {
                    first_error = QString::fromStdString(message);
                }
            }
        }

        self->model_->refresh();

        if (failure_count == 0) {
            QString msg = success_count == 1
                ? "Successfully deleted 1 origin dimension"
                : QString("Successfully deleted %1 origin dimensions").arg(success_count);
            emit self->statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                .arg(failure_count)
                .arg(failure_count == 1 ? "origin dimension" : "origin dimensions")
                .arg(first_error);
            emit self->errorOccurred(msg);
            MessageBoxHelper::critical(self, "Delete Failed", msg);
        } else {
            QString msg = QString("Deleted %1, failed to delete %2")
                .arg(success_count)
                .arg(failure_count);
            emit self->statusChanged(msg);
            MessageBoxHelper::warning(self, "Partial Success", msg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
