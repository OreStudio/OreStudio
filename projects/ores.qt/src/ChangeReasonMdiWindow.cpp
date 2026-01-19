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
#include "ores.qt/ChangeReasonMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QMenu>
#include <QSettings>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ores.qt/ChangeReasonItemDelegate.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.dq/messaging/change_management_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

ChangeReasonMdiWindow::ChangeReasonMdiWindow(
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

QSize ChangeReasonMdiWindow::sizeHint() const {
    return {1000, 500};
}

void ChangeReasonMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);

    setupTable();
    layout->addWidget(tableView_);
}

void ChangeReasonMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    const auto& iconColor = color_constants::icon_color;

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_arrow_sync_20_regular.svg", iconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered, this,
            &ChangeReasonMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, ":/icons/ic_fluent_arrow_sync_20_regular.svg");

    toolbar_->addSeparator();

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_add_20_regular.svg", iconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new change reason"));
    connect(addAction_, &QAction::triggered, this,
            &ChangeReasonMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_edit_20_regular.svg", iconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected change reason"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this,
            &ChangeReasonMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_delete_20_regular.svg", iconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected change reason"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_, &QAction::triggered, this,
            &ChangeReasonMdiWindow::deleteSelected);

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            ":/icons/ic_fluent_history_20_regular.svg", iconColor),
        tr("History"));
    historyAction_->setToolTip(tr("View change reason history"));
    historyAction_->setEnabled(false);
    connect(historyAction_, &QAction::triggered, this,
            &ChangeReasonMdiWindow::viewHistorySelected);
}

void ChangeReasonMdiWindow::setupTable() {
    model_ = new ClientChangeReasonModel(clientManager_, this);
    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    tableView_ = new QTableView(this);
    tableView_->setModel(proxyModel_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::SingleSelection);
    tableView_->setSortingEnabled(true);
    tableView_->setAlternatingRowColors(true);
    tableView_->horizontalHeader()->setStretchLastSection(true);
    tableView_->verticalHeader()->setVisible(false);

    // Set custom delegate for badge rendering
    tableView_->setItemDelegate(new ChangeReasonItemDelegate(this));

    // Set column widths
    tableView_->setColumnWidth(ClientChangeReasonModel::Code, 200);
    tableView_->setColumnWidth(ClientChangeReasonModel::CategoryCode, 120);
    tableView_->setColumnWidth(ClientChangeReasonModel::AppliesToAmend, 100);
    tableView_->setColumnWidth(ClientChangeReasonModel::AppliesToDelete, 100);
    tableView_->setColumnWidth(ClientChangeReasonModel::RequiresCommentary, 120);
    tableView_->setColumnWidth(ClientChangeReasonModel::DisplayOrder, 80);
    tableView_->setColumnWidth(ClientChangeReasonModel::Version, 80);
    tableView_->setColumnWidth(ClientChangeReasonModel::RecordedBy, 120);

    // Setup column visibility with context menu
    setupColumnVisibility();

    // Restore saved settings (column visibility, window size)
    restoreSettings();
}

void ChangeReasonMdiWindow::setupConnections() {
    connect(model_, &ClientChangeReasonModel::dataLoaded,
            this, &ChangeReasonMdiWindow::onDataLoaded);
    connect(model_, &ClientChangeReasonModel::loadError,
            this, &ChangeReasonMdiWindow::onLoadError);

    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &ChangeReasonMdiWindow::onSelectionChanged);
    connect(tableView_, &QTableView::doubleClicked,
            this, &ChangeReasonMdiWindow::onDoubleClicked);
}

void ChangeReasonMdiWindow::reload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading reasons";
    clearStaleIndicator();
    emit statusChanged(tr("Loading reasons..."));
    model_->refresh();
}

void ChangeReasonMdiWindow::onDataLoaded() {
    emit statusChanged(tr("Loaded %1 reasons").arg(model_->rowCount()));
}

void ChangeReasonMdiWindow::onLoadError(const QString& error_message,
                                         const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void ChangeReasonMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void ChangeReasonMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* reason = model_->getReason(sourceIndex.row())) {
        emit showReasonDetails(*reason);
    }
}

void ChangeReasonMdiWindow::updateActionStates() {
    const bool hasSelection = tableView_->selectionModel()->hasSelection();
    editAction_->setEnabled(hasSelection);
    deleteAction_->setEnabled(hasSelection);
    historyAction_->setEnabled(hasSelection);
}

void ChangeReasonMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new change reason requested";
    emit addNewRequested();
}

void ChangeReasonMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* reason = model_->getReason(sourceIndex.row())) {
        emit showReasonDetails(*reason);
    }
}

void ChangeReasonMdiWindow::viewHistorySelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "View history requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* reason = model_->getReason(sourceIndex.row())) {
        BOOST_LOG_SEV(lg(), debug) << "Emitting showReasonHistory for code: "
                                   << reason->code;
        emit showReasonHistory(QString::fromStdString(reason->code));
    }
}

void ChangeReasonMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete change reason while disconnected.");
        return;
    }

    std::vector<std::string> codes;
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* reason = model_->getReason(sourceIndex.row())) {
            codes.push_back(reason->code);
        }
    }

    if (codes.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid change reasons to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << codes.size()
                               << " change reasons";

    QString confirmMessage;
    if (codes.size() == 1) {
        confirmMessage = QString("Are you sure you want to delete change reason '%1'?")
            .arg(QString::fromStdString(codes.front()));
    } else {
        confirmMessage = QString("Are you sure you want to delete %1 change reasons?")
            .arg(codes.size());
    }

    auto reply = MessageBoxHelper::question(this, "Delete Change Reason",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<ChangeReasonMdiWindow> self = this;
    using DeleteResult = std::vector<std::pair<std::string, std::pair<bool, std::string>>>;

    auto task = [self, codes]() -> DeleteResult {
        DeleteResult results;
        if (!self) return {};

        BOOST_LOG_SEV(lg(), debug) << "Making batch delete request for "
                                   << codes.size() << " change reasons";

        dq::messaging::delete_change_reason_request request;
        request.codes = codes;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_change_reason_request,
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

        auto response = dq::messaging::delete_change_reason_response::
            deserialize(*payload_result);

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize batch response";
            for (const auto& code : codes) {
                results.push_back({code, {false, "Invalid server response"}});
            }
            return results;
        }

        for (const auto& result : response->results) {
            results.push_back({result.code, {result.success, result.message}});
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
                BOOST_LOG_SEV(lg(), debug) << "Change reason deleted: " << code;
                success_count++;
                emit self->reasonDeleted(QString::fromStdString(code));
            } else {
                BOOST_LOG_SEV(lg(), error) << "Change reason deletion failed: "
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
                ? "Successfully deleted 1 change reason"
                : QString("Successfully deleted %1 change reasons").arg(success_count);
            emit self->statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                .arg(failure_count)
                .arg(failure_count == 1 ? "change reason" : "change reasons")
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

void ChangeReasonMdiWindow::setupColumnVisibility() {
    QHeaderView* header = tableView_->horizontalHeader();

    // Enable context menu on header for column visibility
    header->setContextMenuPolicy(Qt::CustomContextMenu);
    connect(header, &QHeaderView::customContextMenuRequested,
            this, &ChangeReasonMdiWindow::showHeaderContextMenu);

    // Save header state when sections are moved or resized
    connect(header, &QHeaderView::sectionMoved, this,
            &ChangeReasonMdiWindow::saveSettings);
    connect(header, &QHeaderView::sectionResized, this,
            &ChangeReasonMdiWindow::saveSettings);
}

void ChangeReasonMdiWindow::showHeaderContextMenu(const QPoint& pos) {
    QHeaderView* header = tableView_->horizontalHeader();
    QMenu menu(this);
    menu.setTitle(tr("Columns"));

    // Add action for each column
    for (int col = 0; col < model_->columnCount(); ++col) {
        QString columnName = model_->headerData(col, Qt::Horizontal,
            Qt::DisplayRole).toString();

        QAction* action = menu.addAction(columnName);
        action->setCheckable(true);
        action->setChecked(!header->isSectionHidden(col));

        connect(action, &QAction::toggled, this, [this, header, col](bool visible) {
            header->setSectionHidden(col, !visible);
            saveSettings();
            BOOST_LOG_SEV(lg(), debug) << "Column " << col
                                       << " visibility changed to: " << visible;
        });
    }

    menu.exec(header->mapToGlobal(pos));
}

void ChangeReasonMdiWindow::saveSettings() {
    QSettings settings("OreStudio", "OreStudio");
    settings.beginGroup("ChangeReasonListWindow");

    // Save header state (includes column visibility, order, and widths)
    QHeaderView* header = tableView_->horizontalHeader();
    settings.setValue("headerState", header->saveState());

    // Save window size
    settings.setValue("windowSize", size());

    settings.endGroup();
}

void ChangeReasonMdiWindow::restoreSettings() {
    QSettings settings("OreStudio", "OreStudio");
    settings.beginGroup("ChangeReasonListWindow");

    QHeaderView* header = tableView_->horizontalHeader();

    // Check if we have saved settings
    if (settings.contains("headerState")) {
        // Restore header state
        header->restoreState(settings.value("headerState").toByteArray());
        BOOST_LOG_SEV(lg(), debug) << "Restored header state from settings";
    }

    // Restore window size if saved
    if (settings.contains("windowSize")) {
        resize(settings.value("windowSize").toSize());
    }

    settings.endGroup();
}

}
