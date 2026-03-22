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
#include "ores.qt/ReportDefinitionMdiWindow.hpp"

#include <QVBoxLayout>
#include <QHeaderView>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/ColorConstants.hpp"
#include "ores.reporting/messaging/report_definition_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

ReportDefinitionMdiWindow::ReportDefinitionMdiWindow(
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
      paginationWidget_(nullptr),
      reloadAction_(nullptr),
      addAction_(nullptr),
      editAction_(nullptr),
      deleteAction_(nullptr),
      historyAction_(nullptr),
      scheduleAction_(nullptr),
      unscheduleAction_(nullptr) {

    setupUi();
    setupConnections();

    // Initial load
    reload();
}

void ReportDefinitionMdiWindow::setupUi() {
    auto* layout = new QVBoxLayout(this);

    setupToolbar();
    layout->addWidget(toolbar_);
    layout->addWidget(loadingBar());

    setupTable();
    layout->addWidget(tableView_);

    paginationWidget_ = new PaginationWidget(this);
    layout->addWidget(paginationWidget_);
}

void ReportDefinitionMdiWindow::setupToolbar() {
    toolbar_ = new QToolBar(this);
    toolbar_->setMovable(false);
    toolbar_->setToolButtonStyle(Qt::ToolButtonTextUnderIcon);
    toolbar_->setIconSize(QSize(20, 20));

    reloadAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::ArrowClockwise, IconUtils::DefaultIconColor),
        tr("Reload"));
    connect(reloadAction_, &QAction::triggered, this,
            &EntityListMdiWindow::reload);

    initializeStaleIndicator(reloadAction_, IconUtils::iconPath(Icon::ArrowClockwise));

    toolbar_->addSeparator();

    addAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Add, IconUtils::DefaultIconColor),
        tr("Add"));
    addAction_->setToolTip(tr("Add new report definition"));
    connect(addAction_, &QAction::triggered, this,
            &ReportDefinitionMdiWindow::addNew);

    editAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Edit, IconUtils::DefaultIconColor),
        tr("Edit"));
    editAction_->setToolTip(tr("Edit selected report definition"));
    editAction_->setEnabled(false);
    connect(editAction_, &QAction::triggered, this,
            &ReportDefinitionMdiWindow::editSelected);

    deleteAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::Delete, IconUtils::DefaultIconColor),
        tr("Delete"));
    deleteAction_->setToolTip(tr("Delete selected report definition"));
    deleteAction_->setEnabled(false);
    connect(deleteAction_, &QAction::triggered, this,
            &ReportDefinitionMdiWindow::deleteSelected);

    historyAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::History, IconUtils::DefaultIconColor),
        tr("History"));
    historyAction_->setToolTip(tr("View report definition history"));
    historyAction_->setEnabled(false);
    connect(historyAction_, &QAction::triggered, this,
            &ReportDefinitionMdiWindow::viewHistorySelected);

    toolbar_->addSeparator();

    scheduleAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::CalendarAdd, IconUtils::DefaultIconColor),
        tr("Schedule"));
    scheduleAction_->setToolTip(tr("Schedule selected report definitions"));
    scheduleAction_->setEnabled(false);
    connect(scheduleAction_, &QAction::triggered, this,
            &ReportDefinitionMdiWindow::scheduleSelected);

    unscheduleAction_ = toolbar_->addAction(
        IconUtils::createRecoloredIcon(
            Icon::CalendarCancel, IconUtils::DefaultIconColor),
        tr("Unschedule"));
    unscheduleAction_->setToolTip(tr("Unschedule selected report definitions"));
    unscheduleAction_->setEnabled(false);
    connect(unscheduleAction_, &QAction::triggered, this,
            &ReportDefinitionMdiWindow::unscheduleSelected);
}

void ReportDefinitionMdiWindow::setupTable() {
    model_ = new ClientReportDefinitionModel(clientManager_, this);
    proxyModel_ = new QSortFilterProxyModel(this);
    proxyModel_->setSourceModel(model_);
    proxyModel_->setSortCaseSensitivity(Qt::CaseInsensitive);

    tableView_ = new QTableView(this);
    tableView_->setModel(proxyModel_);
    tableView_->setSelectionBehavior(QAbstractItemView::SelectRows);
    tableView_->setSelectionMode(QAbstractItemView::ExtendedSelection);
    tableView_->setSortingEnabled(true);
    tableView_->setAlternatingRowColors(true);
    tableView_->verticalHeader()->setVisible(false);

    initializeTableSettings(tableView_, model_,
        "ReportDefinitionListWindow",
        {},
        {900, 400}, 1);
}

void ReportDefinitionMdiWindow::setupConnections() {
    connect(model_, &ClientReportDefinitionModel::dataLoaded,
            this, &ReportDefinitionMdiWindow::onDataLoaded);
    connect(model_, &ClientReportDefinitionModel::loadError,
            this, &ReportDefinitionMdiWindow::onLoadError);
    connectModel(model_);

    connect(tableView_->selectionModel(), &QItemSelectionModel::selectionChanged,
            this, &ReportDefinitionMdiWindow::onSelectionChanged);
    connect(tableView_, &QTableView::doubleClicked,
            this, &ReportDefinitionMdiWindow::onDoubleClicked);

    connect(paginationWidget_, &PaginationWidget::page_size_changed,
            this, [this](std::uint32_t size) {
        model_->set_page_size(size);
        model_->refresh();
    });

    connect(paginationWidget_, &PaginationWidget::load_all_requested,
            this, [this]() {
        const auto total = model_->total_available_count();
        if (total > 0 && total <= 1000) {
            model_->set_page_size(total);
            model_->refresh();
        }
    });

    connect(paginationWidget_, &PaginationWidget::page_requested,
            this, [this](std::uint32_t offset, std::uint32_t limit) {
        model_->load_page(offset, limit);
    });
}

void ReportDefinitionMdiWindow::doReload() {
    BOOST_LOG_SEV(lg(), debug) << "Reloading report definitions";
    emit statusChanged(tr("Loading report definitions..."));
    model_->refresh();
}

void ReportDefinitionMdiWindow::onDataLoaded() {
    const auto loaded = model_->rowCount();
    const auto total = model_->total_available_count();
    emit statusChanged(tr("Loaded %1 of %2 report definitions").arg(loaded).arg(total));

    paginationWidget_->update_state(loaded, total);
    paginationWidget_->set_load_all_enabled(
        loaded < static_cast<int>(total) && total > 0 && total <= 1000);
}

void ReportDefinitionMdiWindow::onLoadError(const QString& error_message,
                                          const QString& details) {
    BOOST_LOG_SEV(lg(), error) << "Load error: " << error_message.toStdString();
    emit errorOccurred(error_message);
    MessageBoxHelper::critical(this, tr("Load Error"), error_message, details);
}

void ReportDefinitionMdiWindow::onSelectionChanged() {
    updateActionStates();
}

void ReportDefinitionMdiWindow::onDoubleClicked(const QModelIndex& index) {
    if (!index.isValid())
        return;

    auto sourceIndex = proxyModel_->mapToSource(index);
    if (auto* definition = model_->getDefinition(sourceIndex.row())) {
        emit showDefinitionDetails(*definition);
    }
}

void ReportDefinitionMdiWindow::updateActionStates() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    const int count = selected.size();

    // Single-item actions
    editAction_->setEnabled(count == 1);
    historyAction_->setEnabled(count == 1);

    // Multi-item actions
    deleteAction_->setEnabled(count >= 1);

    // Schedule/unschedule: scan selected rows for active/inactive status
    bool hasInactive = false;
    bool hasActive = false;
    for (const auto& index : selected) {
        const auto sourceIndex = proxyModel_->mapToSource(index);
        if (const auto* def = model_->getDefinition(sourceIndex.row())) {
            if (def->scheduler_job_id.has_value())
                hasActive = true;
            else
                hasInactive = true;
        }
    }
    scheduleAction_->setEnabled(hasInactive);
    unscheduleAction_->setEnabled(hasActive);
}

void ReportDefinitionMdiWindow::addNew() {
    BOOST_LOG_SEV(lg(), debug) << "Add new report definition requested";
    emit addNewRequested();
}

void ReportDefinitionMdiWindow::editSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Edit requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* definition = model_->getDefinition(sourceIndex.row())) {
        emit showDefinitionDetails(*definition);
    }
}

void ReportDefinitionMdiWindow::viewHistorySelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "View history requested but no row selected";
        return;
    }

    auto sourceIndex = proxyModel_->mapToSource(selected.first());
    if (auto* definition = model_->getDefinition(sourceIndex.row())) {
        BOOST_LOG_SEV(lg(), debug) << "Emitting showDefinitionHistory for code: "
                                   << definition->name;
        emit showDefinitionHistory(*definition);
    }
}

void ReportDefinitionMdiWindow::scheduleSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    std::vector<boost::uuids::uuid> ids;
    for (const auto& index : selected) {
        const auto sourceIndex = proxyModel_->mapToSource(index);
        if (const auto* def = model_->getDefinition(sourceIndex.row())) {
            if (!def->scheduler_job_id.has_value())
                ids.push_back(def->id);
        }
    }
    if (!ids.empty())
        emit scheduleRequested(ids);
}

void ReportDefinitionMdiWindow::unscheduleSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    std::vector<boost::uuids::uuid> ids;
    for (const auto& index : selected) {
        const auto sourceIndex = proxyModel_->mapToSource(index);
        if (const auto* def = model_->getDefinition(sourceIndex.row())) {
            if (def->scheduler_job_id.has_value())
                ids.push_back(def->id);
        }
    }
    if (!ids.empty())
        emit unscheduleRequested(ids);
}

void ReportDefinitionMdiWindow::deleteSelected() {
    const auto selected = tableView_->selectionModel()->selectedRows();
    if (selected.isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete requested but no row selected";
        return;
    }

    if (!clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete report definition while disconnected.");
        return;
    }

    std::vector<boost::uuids::uuid> ids;
    std::vector<std::string> codes;  // For display purposes
    for (const auto& index : selected) {
        auto sourceIndex = proxyModel_->mapToSource(index);
        if (auto* definition = model_->getDefinition(sourceIndex.row())) {
            ids.push_back(definition->id);
            codes.push_back(definition->name);
        }
    }

    if (ids.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "No valid report definitions to delete";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete requested for " << ids.size()
                               << " report definitions";

    QString confirmMessage;
    if (ids.size() == 1) {
        confirmMessage = QString("Are you sure you want to delete report definition '%1'?")
            .arg(QString::fromStdString(codes.front()));
    } else {
        confirmMessage = QString("Are you sure you want to delete %1 report definitions?")
            .arg(ids.size());
    }

    auto reply = MessageBoxHelper::question(this, "Delete Report Definition",
        confirmMessage, QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<ReportDefinitionMdiWindow> self = this;
    using DeleteResult = std::vector<std::tuple<boost::uuids::uuid, std::string, bool, std::string>>;

    auto task = [self, ids, codes]() -> DeleteResult {
        DeleteResult results;
        if (!self) return {};

        BOOST_LOG_SEV(lg(), debug) << "Making batch delete request for "
                                   << ids.size() << " report definitions";

        reporting::messaging::delete_report_definition_request request;
        for (const auto& id : ids) {
            request.ids.push_back(boost::uuids::to_string(id));
        }
        auto response_result = self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to send batch delete request";
            for (std::size_t i = 0; i < ids.size(); ++i) {
                results.push_back({ids[i], codes[i], false, "Failed to communicate with server"});
            }
            return results;
        }

        for (std::size_t i = 0; i < ids.size(); ++i) {
            std::string code = (i < codes.size()) ? codes[i] : "";
            results.push_back({ids[i], code, response_result->success, response_result->message});
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

        for (const auto& [id, code, success, message] : results) {
            if (success) {
                BOOST_LOG_SEV(lg(), debug) << "Report Definition deleted: " << code;
                success_count++;
                emit self->definitionDeleted(QString::fromStdString(code));
            } else {
                BOOST_LOG_SEV(lg(), error) << "Report Definition deletion failed: "
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
                ? "Successfully deleted 1 report definition"
                : QString("Successfully deleted %1 report definitions").arg(success_count);
            emit self->statusChanged(msg);
        } else if (success_count == 0) {
            QString msg = QString("Failed to delete %1 %2: %3")
                .arg(failure_count)
                .arg(failure_count == 1 ? "report definition" : "report definitions")
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
